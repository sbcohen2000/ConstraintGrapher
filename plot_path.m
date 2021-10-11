
x_axis = linspace(-3, 3, 1000);
y_axis = linspace(-3, 3, 1000);
z = zeros(length(y_axis), length(x_axis));

for xx = (1:length(x_axis))
  for yy = (1:length(y_axis))
    x = x_axis(xx);
    y = y_axis(yy);
    a = sqrt(x^2 + y^2) - 2;
    b = 0;
    z(yy, xx) = a^2 + b^2;
  end
end

imagesc(x_axis, y_axis, log10(z / max(max(z))));
colormap gray;
hold on;

% Draw path

raw = readmatrix('data.csv');
raw = raw(1:1,:);

x = raw(:,1);
y = raw(:,2);
selected_x = raw(:,3);
selected_y = raw(:,4);
rest = raw(:,5:end);
points = zeros(height(raw) * 5, 2);

idxs = 1:5:height(rest)*5;
for k = 1:height(rest)
    idx = idxs(k);
    for r = 0:4
        points(idx + r, 1) = rest(k, 1 + r * 2);
        points(idx + r, 2) = rest(k, 2 + r * 2);
    end
end

plot(points(:,1), points(:,2), 'b*');
hold on;
plot(x, y, 'r*');
hold on
plot(x, y, 'gO');

xlabel('x_0');
ylabel('x_1');
