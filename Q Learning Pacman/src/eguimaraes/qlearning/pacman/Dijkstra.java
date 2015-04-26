package eguimaraes.qlearning.pacman;

import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import eguimaraes.qlearning.pacman.Maze.Position;

//http://www.algolist.com/code/java/Dijkstra's_algorithm

class Vertex implements Comparable<Vertex> {
	public int x;
	public int y;
	public Edge[] adjacencies;
	public double minDistance = Double.POSITIVE_INFINITY;
	public Vertex previous;

	public Vertex(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public String toString() {
		return Integer.toString(x) + " " + Integer.toString(y) ;
	}

	public int compareTo(Vertex other) {
		return Double.compare(minDistance, other.minDistance);
	}
}

class Edge {
	public final Vertex target;
	public final double weight = 1;

	public Edge(Vertex argTarget) {
		target = argTarget;
	}
}

public class Dijkstra {


	private ArrayList<Vertex> mazeToGraph(Maze maze) {
		return createEdges(mazeToVertexs(maze), maze);
	}

	private ArrayList<Vertex> mazeToVertexs(Maze maze) {
		ArrayList<Vertex> graph = new ArrayList<>();
		for (int i = 0; i < maze.WIDTH; i++) {
			for (int j = 0; j < maze.HEIGHT; j++) {
				if (!maze.isBlocked(i, j))
					graph.add(new Vertex(i, j));
			}
		}
		return graph;
	}

	private ArrayList<Vertex> createEdges(ArrayList<Vertex> list, Maze maze) {
		Maze.Position[] positions;
		for (Vertex vertex : list) {
			positions = maze.getNeighbors(vertex.x, vertex.y);
			if (positions != null) {
//				System.out.println(vertex.toString());
//				for (Position p : positions) {
//					System.out.println(p.toString());
//				}
				Edge[] edges = new Edge[positions.length];
				for (int i = 0; i < positions.length; i++) {
					Vertex v = getVertexByPos(list, positions[i].x,	positions[i].y);
					if(v!=null)
					 edges[i] = new Edge(v);
				}
				vertex.adjacencies = edges;
			}
		}
		return list;
	}

	private Vertex getVertexByPos(ArrayList<Vertex> list, int x, int y) {
		for (Vertex v : list) {
			if (v.x == x && v.y == y)
				return v;
		}
		return null;
	}

	private void computePaths(Vertex source) {
		source.minDistance = 0.;
		PriorityQueue<Vertex> vertexQueue = new PriorityQueue<Vertex>();
		vertexQueue.add(source);

		while (!vertexQueue.isEmpty()) {
			Vertex u = vertexQueue.poll();

			// Visit each edge exiting u
			for (Edge e : u.adjacencies) {
				Vertex v = e.target;
				double weight = e.weight;
				double distanceThroughU = u.minDistance + weight;
				if (distanceThroughU < v.minDistance) {
					vertexQueue.remove(v);
					v.minDistance = distanceThroughU;
					v.previous = u;
					vertexQueue.add(v);
				}
			}
		}
	}

	private List<Vertex> getShortestPathTo(Vertex target) {
		List<Vertex> path = new ArrayList<Vertex>();
		for (Vertex vertex = target; vertex != null; vertex = vertex.previous)
			path.add(vertex);
		Collections.reverse(path);
		return path;
	}

	public int getDistanceToTheClosestDot(int x, int y, Maze maze) {
		ArrayList<Vertex> mazeGraph = mazeToGraph(maze);
		computePaths(getVertexByPos(mazeGraph, x, y));
		
		int disClosestDot = maze.WIDTH * maze.HEIGHT;
		for (int i = 0; i < maze.WIDTH; i++) {
			for (int j = 0; j < maze.HEIGHT; j++) {
				Vertex v = getVertexByPos(mazeGraph, i, j);
				if (v != null && maze.isDot(i, j))
					if (v.minDistance < disClosestDot)
						disClosestDot = (int) v.minDistance;
			}
		}
		return disClosestDot;
	}
	
	
	public int getDistanceToTheClosestEatableGhost(int x, int y, Ghost[] ghosts, Maze maze) {
		ArrayList<Vertex> mazeGraph = mazeToGraph(maze);
		computePaths(getVertexByPos(mazeGraph, x, y));
		
		int disClosestGhost = maze.WIDTH * maze.HEIGHT;
		for (int i = 0; i < ghosts.length; i++) {
				Vertex v = getVertexByPos(mazeGraph, FeaturesExtraction.toHouseSize(ghosts[i].iX), FeaturesExtraction.toHouseSize(ghosts[i].iX));
				if (v != null && ghosts[i].iStatus == Ghost.BLIND)
					if (v.minDistance < disClosestGhost)
						disClosestGhost = (int) v.minDistance;
		}
		return disClosestGhost==maze.WIDTH * maze.HEIGHT?0:disClosestGhost;
	}

	/*
	 * public static void main(String[] args) {
	 * 
	 * Vertex v0 = new Vertex(0, false); Vertex v1 = new Vertex(1, false);
	 * Vertex v2 = new Vertex(2, false); Vertex v3 = new Vertex(3, false);
	 * Vertex v4 = new Vertex(4, false);
	 * 
	 * v0.adjacencies = new Edge[]{ new Edge(v1, 5), new Edge(v2, 10), new
	 * Edge(v3, 8) }; v1.adjacencies = new Edge[]{ new Edge(v0, 5), new Edge(v2,
	 * 3), new Edge(v4, 7) }; v2.adjacencies = new Edge[]{ new Edge(v0, 10), new
	 * Edge(v1, 3) }; v3.adjacencies = new Edge[]{ new Edge(v0, 8), new Edge(v4,
	 * 2) }; v4.adjacencies = new Edge[]{ new Edge(v1, 7), new Edge(v3, 2) };
	 * Vertex[] vertices = { v0, v1, v2, v3, v4 }; computePaths(v0); for (Vertex
	 * v : vertices) { System.out.println("Distance to " + v + ": " +
	 * v.minDistance); List<Vertex> path = getShortestPathTo(v);
	 * System.out.println("Path: " + path); } }
	 */
}
